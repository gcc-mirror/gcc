/* { dg-do run } */
/* { dg-options "-O3 ${srcdir}/g++.dg/ipa/pr64049-1.C" } */

#include "pr64049.h"

int
main ()
{
	ValueStruct v;
	v.arrayType = 0;
	v.dataType = 0;
	v.value.LocalizedText = new LocalizedTextStruct ("Localized Text");
	LocalizedText t = ValueHelper::getLocalizedText (&v);
	if (__builtin_strcmp (t.getInternHandle ()->getT (), "Localized Text"))
		__builtin_abort ();
	return 0;
}

LocalizedTextStruct*
LocalizedText::getInternHandle ()
{
	return &t;
}
