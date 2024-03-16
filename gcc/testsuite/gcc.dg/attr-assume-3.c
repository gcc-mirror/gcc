/* Portable assumptions */
/* { dg-do compile } */
/* { dg-options "-std=c23" } */

void
foo (int x)
{
  if (x == 1)
    goto l1;						/* { dg-error "jump into statement expression" } */
  else if (x == 2)
    goto l2;						/* { dg-error "jump into statement expression" } */
  else if (x == 3)
    goto l3;						/* { dg-error "jump into statement expression" } */
  [[gnu::assume (({ l0:; if (x == 0) goto l0; 1; }))]];
  [[gnu::assume (({ if (x == 0) __builtin_abort (); 1; }))]];
  [[gnu::assume (({ l1:; 1; }))]];			/* { dg-message "label 'l1' defined here" } */
  [[gnu::assume (({ l2:; 1; }))]];			/* { dg-message "label 'l2' defined here" } */
  __attribute__((assume (({ l3:; 1; }))));		/* { dg-message "label 'l3' defined here" } */
  [[gnu::assume (({ l4:; 1; }))]];			/* { dg-message "label 'l4' defined here" } */
  [[gnu::assume (({ l5:; 1; }))]];			/* { dg-message "label 'l5' defined here" } */
  __attribute__((assume (({ l6:; 1; }))));		/* { dg-message "label 'l6' defined here" } */
  switch (x)						/* { dg-message "switch starts here" } */
    {
    case 7:
      [[gnu::assume (({ case 8:; 1; }))]];		/* { dg-error "switch jumps into statement expression" } */
      __attribute__((assume (({ default:; 1; }))));	/* { dg-error "switch jumps into statement expression" } */
      break;
    }
  if (x == 4)
    goto l4;						/* { dg-error "jump into statement expression" } */
  else if (x == 5)
    goto l5;						/* { dg-error "jump into statement expression" } */
  else if (x == 6)
    goto l6;						/* { dg-error "jump into statement expression" } */
}
