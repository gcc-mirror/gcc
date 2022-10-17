/* PERMUTE_ARGS: -O
 * If not careful, this can produce exponential tree traversal times
 * when compiling the generated opEquals() function.
 */

struct Param
{
    bool verbose;
    bool vcg_ast;
    bool showColumns;
    bool vtls;
    bool vtemplates;
    bool vgc;
    bool vfield;
    bool vcomplex;
    int useDeprecated;
    bool stackstomp;
    bool useUnitTests;
    bool useInline;
    bool useDIP25;
    bool noDIP25;
    bool useDIP1021;
    bool release;
    bool a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z;
    uint debuglevel;
    void* debugids;

    uint versionlevel;
    void* versionids;

    const(char)[] defaultlibname;
    const(char)[] debuglibname;
    const(char) mscrtlib;

    void* moduleDeps;
    int messageStyle = 1;
}

struct Global
{
    Param params;
}
