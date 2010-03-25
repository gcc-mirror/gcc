#include <stdlib.h>
#include "../../objc-obj-c++-shared/next-mapping.h"
#ifndef __NEXT_RUNTIME__
#include <objc/encoding.h>
#endif

void print_ivars (Class class)
{
  struct objc_ivar_list* ivars = class->ivars;
  int i;

  for (i = 0; i < ivars->ivar_count; i++) {
    struct objc_ivar *ivar = &(ivars->ivar_list[i]);
    printf ("ivar '%s', type '%s', offset %d\n",
	    ivar->ivar_name, ivar->ivar_type, ivar->ivar_offset);
  }
}

void compare_structures (Class class, const char* type)
{
  struct objc_struct_layout layout;
  struct objc_ivar_list* ivars = class->ivars;
  int i = 0;
  int position;

  objc_layout_structure (type, &layout);

  while (objc_layout_structure_next_member (&layout))
    {
      struct objc_ivar *ivar;
      const char *ivar_type;

      if (i > ivars->ivar_count)
        {
          printf ("too many ivars in type %s, layout = %s\n",
                  type, layout.type);
          exit (1);
        }

      ivar = &(ivars->ivar_list[i]);
      objc_layout_structure_get_info (&layout, &position, NULL, &ivar_type);
      printf ("real ivar '%s' offset %d\n",
              ivar->ivar_name, ivar->ivar_offset);
      printf ("computed type '%s' offset %d\n", ivar_type, position);
      if (position != ivar->ivar_offset)
        {
          printf ("offset %d and computed position %d don't match on ivar '%s'"
                  " (i = %d)\n",
                  ivar->ivar_offset, position, ivar->ivar_name, i);
          exit (1);
        }
      i++;
    }
  
  printf ("%d ivars checked\n", i);
}

int main ()
{
  struct class_vars
    {
      @defs (MyObject);
    };
  int size1, size2;
  Class class = objc_get_class ("MyObject");

  printf ("type = %s\n", @encode (struct class_vars));
  print_ivars (class);

  compare_structures (class, @encode(struct class_vars));
  if ((size1 = objc_sizeof_type (@encode(struct class_vars)))
      != (size2 = sizeof (struct class_vars)))
    {
      printf ("sizes don't match (computed %d, exact %d)\n", size1, size2);
      abort ();
    }
  
  exit (0);
}
