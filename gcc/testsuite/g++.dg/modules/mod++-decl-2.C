// { dg-options "-fmodules++" }

export module thing;
int i;
module // { dg-error "expected" }
{
}
