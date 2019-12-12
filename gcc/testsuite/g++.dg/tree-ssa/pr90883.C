// { dg-options "-O2 -Os -fdump-tree-dse-details -std=c++11" }


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
// { dg-final { scan-tree-dump "Deleted redundant store: .*\.a = {}" "dse1" { target { ! i?86-*-* } } } }
// { dg-final { scan-tree-dump "Deleted redundant store: .*\.a = {}" "dse2" { target i?86-*-* } } }

