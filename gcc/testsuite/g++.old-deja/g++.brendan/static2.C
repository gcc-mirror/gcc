// { dg-do assemble  }
// GROUPS passed static
class A
{
        public:
        void    member(void)
        {
        }

        static void staticMember()
        {
	  member (); // illegal, no object for calling non-static method// { dg-error "" } .*
        }
};

int main()
{
        A::staticMember();
}
