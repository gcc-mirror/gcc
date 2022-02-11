// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh

module ddoc11479;

///
struct S1(T)
{
    ///
    int a;

    ///
private:
    int x;

private:
    ///
    int y;

    ///
public:
    int b;

public:
    ///
    int c;
}


///
struct S2(T)
{
    ///
    int a;

    ///
    private int x;

    ///
    int b;

    ///
    public int c;

    public
    ///
    int d;
}


///
struct S3(T)
{
    ///
    int a;

    ///
    private { int x; }

    ///
    int b;

    ///
    private
    {
        int y;

        public
        {
            int c;
        }
    }

    private
    {
        int z;

        ///
        public
        {
            int d;
        }
    }

    private
    {
        int w;

        public
        {
            ///
            int e;
        }
    }
}
