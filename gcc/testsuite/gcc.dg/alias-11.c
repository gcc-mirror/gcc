/* { dg-do run } */
/* { dg-require-alias "" } */
/* { dg-options "-O2" } */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct dw_cfi_struct
{
  struct dw_cfi_struct *dw_cfi_next;
  const char *dw_cfi_addr;
}
dw_cfi_node;

typedef struct dw_fde_struct
{
  const char *dw_fde_current_label;
  dw_cfi_node *dw_fde_cfi;
}
dw_fde_node;

dw_cfi_node *cie_cfi_head;
unsigned fde_table_in_use;
dw_fde_node *fde_table;

static __inline__ void
add_cfi (dw_cfi_node **list_head, dw_cfi_node *cfi)
{
  dw_cfi_node **p;

  for (p = list_head; (*p) != ((void *)0); p = &(*p)->dw_cfi_next)
    ;

  *p = cfi;
}

static __inline__ struct dw_cfi_struct *
new_cfi (void)
{
  dw_cfi_node *cfi = (dw_cfi_node *) malloc (sizeof (dw_cfi_node));

  memset (cfi, 0, sizeof (dw_cfi_node));
  return cfi;
}

char *
dwarf2out_cfi_label (void)
{
  static char label[20];
  static unsigned long label_num = 0;

  sprintf (label, "*.%s%u", "LCFI", (unsigned) (label_num++));
  return label;
}

void
add_fde_cfi (const char *label, dw_cfi_node *cfi)
{
  if (label)
    {
      dw_fde_node *fde = fde_table + fde_table_in_use - 1;

      if (*label == 0)
	label = dwarf2out_cfi_label ();

      if (fde->dw_fde_current_label == ((void *)0)
	  || strcmp (label, fde->dw_fde_current_label))
	{
	  dw_cfi_node *xcfi;

	  fde->dw_fde_current_label = label = strdup (label);

	  xcfi = new_cfi ();
	  xcfi->dw_cfi_addr = label;
	  add_cfi (&fde->dw_fde_cfi, xcfi);
	}

      add_cfi (&fde->dw_fde_cfi, cfi);
    }
  else
    add_cfi (&cie_cfi_head, cfi);
}

int
main ()
{
  dw_cfi_node *cfi;
  dw_fde_node *fde;

  fde_table_in_use = 1;
  fde_table = (dw_fde_node *) realloc (fde_table,
				       sizeof (dw_fde_node));
  memset (fde_table, 0, sizeof (dw_fde_node));
  cfi = new_cfi ();
  add_fde_cfi ("", cfi);

  fde = &fde_table[0];
  cfi = fde->dw_fde_cfi;

  if (cfi == NULL)
    abort ();

  if (cfi->dw_cfi_addr == NULL)
    abort ();

  if (strcmp ("*.LCFI0", cfi->dw_cfi_addr))
    abort ();

  return 0;
}
