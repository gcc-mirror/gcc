/*
 * Copyright (C) 2008 Free Software Foundation, Inc.
 * This file is part of GNU Modula-2.
 *  
 * GNU Modula-2 is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2, or (at your option) any later
 * version.
 * 
 * GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with gm2; see the file COPYING.  If not, write to the Free Software
 * Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include <setjmp.h>
#include <malloc.h>
#include <stdio.h>

typedef enum jmpstatus {
  jmp_normal,
  jmp_retry,
  jmp_exception,
} jmp_status;

struct setjmp_stack {
  jmp_buf  env;
  struct setjmp_stack *next;
} *head = NULL;

void pushsetjmp (void)
{
  struct setjmp_stack *p = (struct setjmp_stack *) malloc (sizeof (struct setjmp_stack));

  p->next = head;
  head = p;
}

void exception (void)
{
  printf("invoking exception handler\n");
  longjmp (head->env, jmp_exception);
}

void retry (void)
{
  printf("retry\n");
  longjmp (head->env, jmp_retry);
}

void popsetjmp (void)
{
  struct setjmp_stack *p = head;

  head = head->next;
  free (p);
}


static int *ip = NULL;

void fly (void)
{
  printf("fly main body\n");
  if (ip == NULL) {
    printf("ip == NULL\n");
    exception();
  }
  if ((*ip) == 0) {
    printf("*ip == 0\n");
    exception();
  }
  if ((4 / (*ip)) == 4)
    printf("yes it worked\n");
  else
    printf("no it failed\n");
}

/*
 *   a GNU C version of the Modula-2 example given in the ISO standard.
 *   This is written to prove that the underlying runtime system
 *   will work with the C interpretation.  Thus gm2 will produce
 *   trees which follow the "template" setjmp/longjmp schema below
 *   when compiling EXCEPT/TRY statements.
 */

void tryFlying (void)
{
  void tryFlying_m2_exception () {
    printf("inside tryFlying exception routine\n");
    if ((ip != NULL) && ((*ip) == 0)) {
      (*ip) = 1;
      retry();
    }
  }

  int t;

  pushsetjmp ();
  do {
    t = setjmp (head->env);
  } while (t == jmp_retry);

  if (t == jmp_exception) {
    /* exception called */
    tryFlying_m2_exception ();
    /* exception has not been handled, invoke previous handler */
    printf("exception not handled here\n");
    popsetjmp();
    exception();
  }

  printf("tryFlying main body\n");  
  fly();
  popsetjmp();
}


void keepFlying (void)
{
  void keepFlying_m2_exception () {
    printf("inside keepFlying exception routine\n");
    if (ip == NULL) {
      ip = (int *)malloc (sizeof (int));
      *ip = 0;
      retry();
    }
  }
  int t;

  pushsetjmp ();
  do {
    t = setjmp (head->env);
  } while (t == jmp_retry);
  
  if (t == jmp_exception) {
    /* exception called */
    keepFlying_m2_exception ();
    /* exception has not been handled, invoke previous handler */
    popsetjmp();
    exception();
  }
  printf("keepFlying main body\n");
  tryFlying();
  popsetjmp();
}


main ()
{
  keepFlying();
  printf("all done\n");
}
