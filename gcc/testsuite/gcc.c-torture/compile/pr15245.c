/*  Testcase from <marcus@jet.franken.de>
    PR optimization/15245
    This used to ICE as convert was used
    in tree-ssa-phiopt which created non gimple
    code.   */

char *f(char *x, int flag)
{
    char *ret = (char*)0;


    if( x > (char*)1 ) {
      if(x)
        return (char*)0;
    } else {
      if( flag & 1 )
        ret = (char*)1;
      flag |= 2;
    }
    return ret;
}
