// { dg-options "-O2 -Os -fdump-tree-dse-details -std=c++11 --param max-inline-insns-size=1" }


    class C
    {
        char a[7]{};
        int b{};
    };

    C slow()
    {
        return {};
    }


// We want to match enough here to capture that we deleted an empty
// constructor store
// mips will expand to loop to clear because CLEAR_RATIO.
// { dg-final { scan-tree-dump-not ".*\.a = {}" "dse1" { xfail { mips*-*-* } } } }
// { dg-final { scan-tree-dump-not ".*\.b = 0" "dse1" { xfail { mips*-*-* } } } }
