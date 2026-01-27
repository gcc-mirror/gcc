// { dg-do compile }
module pr122817;

struct object {} // { dg-error "conflicts with import 'pr122817.object' at .*pr122817.d" }
