/* Copyright (C) 2001  Free Software Foundation.
   by Hans-Peter Nilsson  <hp@axis.com>

   Making sure that invalid asm operand modifiers don't cause an ICE.  */

/* { dg-do compile { target cris-*-* } } */
/* { dg-options "-O2" } */
/* { dg-error "reg:SI|const_double:DF" "prune debug_rtx output" { target cris-*-* } 0 } */

void
foo (void)
{
  /* The first case symbolizes the default case for CRIS.  */
  asm ("\n;# %w0" : : "r" (0));	/* { dg-error "modifier" } */

  /* These are explicit cases.  Luckily, a register is invalid in most of
     them.  */
  asm ("\n;# %b0" : : "r" (0));		/* { dg-error "modifier" } */
  asm ("\n;# %v0" : : "r" (0));		/* { dg-error "modifier" } */
  asm ("\n;# %P0" : : "r" (0));		/* { dg-error "modifier" } */
  asm ("\n;# %p0" : : "r" (0));		/* { dg-error "modifier" } */
  asm ("\n;# %z0" : : "r" (0));		/* { dg-error "modifier" } */
  asm ("\n;# %H0" : : "F" (0.5));	/* { dg-error "modifier" } */
  asm ("\n;# %e0" : : "r" (0));		/* { dg-error "modifier" } */
  asm ("\n;# %m0" : : "r" (0));		/* { dg-error "modifier" } */
  asm ("\n;# %A0" : : "r" (0));		/* { dg-error "modifier" } */
  asm ("\n;# %D0" : : "r" (0));		/* { dg-error "modifier" } */
  asm ("\n;# %T0" : : "r" (0));		/* { dg-error "modifier" } */
  /* Add more must-not-ICE asm errors here as we find them ICEing.  */
}
