// { dg-do assemble  }
// GROUPS passed scoping
// local-class file
// From: schlaege@methusalix.ert.rwth-aachen.de (Chris Schlaeger H Zivojnovic)
// Date:     Tue, 10 Aug 93 16:50:33 +0200
// Subject:  Bug report
// Message-ID: <9308101450.AA28016@methusalix.ert.rwth-aachen.de>

int main()
{
        class foo
        {
                int i;
        } ;
        class bar
        {
                public:
                        bar() { y = 0; }
                        void f() { foo x; }
                private:
                        int y;
        } ;

        bar     c;
}
