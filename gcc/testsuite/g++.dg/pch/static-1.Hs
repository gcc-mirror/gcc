class A
{
public:
        int val;

        ~A() {
                int i = 2;
        }

        static void StaticTest() {
                static A a;
        }
};
