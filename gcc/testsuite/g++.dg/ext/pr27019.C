
// { dg-do compile }
// { dg-options "" }

struct A
{
            int i;
                int z[1];
};

A a = { z:{} }; // { dg-message "unimplemented" }
