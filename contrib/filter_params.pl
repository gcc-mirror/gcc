#!/usr/bin/perl

# Filters out some of the #defines used thourghout the GCC sources:
# - GTY(()) marks declarations for gengtype.c
# - PARAMS(()) is used for K&R compatibility. See ansidecl.h.

while (<>) {
    s/^\/\* /\/\*\* \@verbatim /;
    s/\*\// \@endverbatim \*\//;
    s/GTY[ \t]*\(\(.*\)\)//g;
    s/[ \t]ATTRIBUTE_UNUSED//g;
    s/PARAMS[ \t]*\(\((.*?)\)\)/\($1\)/sg;
    print;
}
