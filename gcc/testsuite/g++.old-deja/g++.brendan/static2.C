// Build don't link: 
// GROUPS passed static
class A
{
        public:
        void    member(void)
        {
        }

        static void staticMember()
        {
	  member (); // illegal, no object for calling non-static method// ERROR - .*
        }
};

int main()
{
        A::staticMember();
}
