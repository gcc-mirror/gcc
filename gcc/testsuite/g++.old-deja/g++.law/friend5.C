// { dg-do assemble  }
// { dg-options "-Wreturn-type" }
// GROUPS passed friends

        extern "C" int good_friend(int);
        extern "C" int bad_friend();

        class assembler {
            friend int good_friend(int);
            friend int bad_friend();
            void *parts;
        };

        assembler obj;

        int good_friend(int)
        {
            obj.parts = 0;
        } // { dg-error "" } non-void

        int bad_friend()
        {
            obj.parts = 0;
        } // { dg-error "" } non-void

