/* { dg-require-effective-target lto } */
/* { dg-additional-sources "afdo-crossmodule-1.c" } */
/* { dg-options "-O3 -flto -fdump-ipa-afdo_offline -fdump-tree-einline-details" } */
/* { dg-require-profiling "-fauto-profile" } */

extern int foo2 ();

int bar (int (*fooptr) (int (*)()))
{
	return fooptr (foo2);
}
/* { dg-final-use-autofdo { scan-ipa-dump "Offlining function inlined to other module: main:5 bar" "afdo_offline"} } */
/* { dg-final-use-autofdo { scan-ipa-dump "Offlining function inlined to other module: bar:2 main:5 foo" "afdo_offline"} } */
/* It would be nice to speculate call to foo, but offlining does not preserve jump target
   and currently afdo does not do cross-module indirect call promotion.  */
