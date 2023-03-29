// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
void main()
{
    align(128) byte var;
    assert((cast(size_t) &var) % 128 == 0);
    var = 73;
    assert((() => var)() == 73);
}
