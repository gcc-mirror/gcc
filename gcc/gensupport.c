/* Support routines for the various generation passes.
   Copyright (C) 2000 Free Software Foundation, Inc.

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU CC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#include "hconfig.h"
#include "system.h"
#include "rtl.h"
#include "obstack.h"
#include "errors.h"
#include "gensupport.h"


static struct obstack obstack;
struct obstack *rtl_obstack = &obstack;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

static FILE *input_file;

static int sequence_num;

struct queue_elem {
    rtx data;
    struct queue_elem *next;
};

static struct queue_elem *rtx_ready_queue;

static void remove_constraints PARAMS ((rtx));
static void process_rtx PARAMS ((rtx *));

void
message_with_line VPARAMS ((int lineno, const char *msg, ...))
{
#ifndef ANSI_PROTOTYPES
  int lineno;
  const char *msg;
#endif
  va_list ap;

  VA_START (ap, msg);

#ifndef ANSI_PROTOTYPES
  lineno = va_arg (ap, int);
  msg = va_arg (ap, const char *);
#endif

  fprintf (stderr, "%s:%d: ", read_rtx_filename, lineno);
  vfprintf (stderr, msg, ap);
  fputc ('\n', stderr);

  va_end (ap);
}

/* Recursively remove constraints from an rtx.  */

static void
remove_constraints (part)
     rtx part;
{
  register int i, j;
  register const char *format_ptr;

  if (part == 0)
    return;

  if (GET_CODE (part) == MATCH_OPERAND)
    XSTR (part, 2) = "";
  else if (GET_CODE (part) == MATCH_SCRATCH)
    XSTR (part, 1) = "";

  format_ptr = GET_RTX_FORMAT (GET_CODE (part));

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (part)); i++)
    switch (*format_ptr++)
      {
      case 'e':
      case 'u':
	remove_constraints (XEXP (part, i));
	break;
      case 'E':
	if (XVEC (part, i) != NULL)
	  for (j = 0; j < XVECLEN (part, i); j++)
	    remove_constraints (XVECEXP (part, i, j));
	break;
      }
}

/* Handle any synthetic top level rtx, i.e. anything except:
       DEFINE_INSN
       DEFINE_EXPAND
       DEFINE_SPLIT
       DEFINE_PEEPHOLE
       DEFINE_PEEPHOLE2
       DEFINE_ATTRIBUTE
       DEFINE_FUNCTION_UNIT
       DEFINE_ASM_ATTRIBUTES */

static void
process_rtx (desc)
    rtx* desc;
{
  if (GET_CODE (*desc) == DEFINE_INSN_AND_SPLIT) 
    {
      struct queue_elem* elem = xmalloc (sizeof (struct queue_elem));
      const char *split_cond;
  
      /* Create a split with values from the insn_and_split. */
      rtx split = rtx_alloc (DEFINE_SPLIT);
      XEXP (split, 0) = copy_rtx (XEXP (*desc, 1));
      remove_constraints (XEXP (split, 0));
      split_cond = XSTR (split, 1) = XSTR (*desc, 4);
  
      /* If the split condition starts with "&&", append it to the
         insn condition to create the new split condition.  */
      if (split_cond[0] == '&' && split_cond[1] == '&')
        {
	const char *insn_cond = XSTR (*desc, 2);
  	char *combined = 
  	    xmalloc (strlen (insn_cond) + strlen (split_cond) + 1);
  	strcpy (combined, insn_cond);
  	strcat (combined, split_cond);
  	XSTR (split, 1) = combined;
        }
  
      XVEC (split, 2) = XVEC (*desc, 5);
      XSTR (split, 3) = XSTR (*desc, 6);
  
      /* Fix up the DEFINE_INSN.  */
      PUT_CODE (*desc, DEFINE_INSN);
      XVEC (*desc, 4) = XVEC (*desc, 7);
  
      /* Return the DEFINE_INSN part, and put the DEFINE_SPLIT
         in the queue.  */
      elem->next = rtx_ready_queue;
      elem->data = split;	
      rtx_ready_queue = elem;  
    }
}

/* The entry point for initializing the reader.  */

int 
init_md_reader (filename)
    const char *filename;
{
  read_rtx_filename = filename;
  input_file = fopen (filename, "r");
  if (input_file == 0)
    {
      perror (filename);
      return FATAL_EXIT_CODE;
    }

  obstack_init (rtl_obstack);
  sequence_num = 0;
  rtx_ready_queue = NULL; 

  return SUCCESS_EXIT_CODE;
}


/* The entry point for reading a single rtx from an md file.  */

rtx 
read_md_rtx (lineno, seqnr)
    int *lineno;
    int *seqnr;
{ 
  rtx desc;

  if (rtx_ready_queue != NULL) 
    {
      desc = rtx_ready_queue->data;
      rtx_ready_queue = rtx_ready_queue->next;
    }
  else 
    {
      int c;
      c = read_skip_spaces (input_file);
      if (c == EOF)
	return NULL;

      ungetc (c, input_file);
      desc = read_rtx (input_file);
      process_rtx (&desc);
    }
  *lineno = read_rtx_lineno;
  *seqnr = sequence_num;
  switch (GET_CODE (desc))
    {
      case DEFINE_INSN:
      case DEFINE_EXPAND:
      case DEFINE_SPLIT:
      case DEFINE_PEEPHOLE:
      case DEFINE_PEEPHOLE2:
	sequence_num++;
	break;

      default:
	break;
    }

  return desc;
}
