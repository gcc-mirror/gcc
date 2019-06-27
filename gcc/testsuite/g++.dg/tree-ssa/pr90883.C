// { dg-options "-O2 -fdump-tree-dse1-details -std=c++11" }


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
// { dg-final { scan-tree-dump "Deleted redundant store: .*\.a = {}" "dse1" } }

