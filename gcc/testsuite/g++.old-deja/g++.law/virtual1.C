// Build don't link: 
// Special g++ Options: -Woverloaded-virtual
// GROUPS passed virtual-warnings
// copy file
// From: gfm@mencon.mencon.oz.au (Graham Menhennitt)
// Date:     Thu, 29 Apr 93 20:53:07 EST
// Subject:  4 bugs in g++ 2.3.3
// Message-ID: <9304291053.AA00090@mencon>

        struct A {
                virtual ~A(void);
        };

        struct B {
                friend class A;
                virtual void f(void);
        };

        struct C : public A {
                virtual void f(void);
        };
