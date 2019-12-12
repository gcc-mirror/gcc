// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh 5

/**

  Test module

*/
module test;

/// class to test DDOC on members
class TestMembers(TemplateArg)
{
  public:
    /**

       a static method 

       Params: idx = index
   
    */
    static void PublicStaticMethod(int  idx)
    {
    }
}

void main()
{
}

