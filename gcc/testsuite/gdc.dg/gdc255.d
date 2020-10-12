// https://bugzilla.gdcproject.org/show_bug.cgi?id=255
// { dg-do compile }

class C255
{
    void f2()
    {
        class C1
        {
            void f1()
            {
                void f0()
                {
                    class C0
                    {
                        void test255()
                        {
                            f2();
                        }
                    }
                }
            }
        }
    }
}

class C255a
{
    void f3()
    {
        class C1
        {
            void f2()
            {
                void f1()
                {
                    void f0()
                    {
                        class C0
                        {
                            void test255a()
                            {
                                f3();
                            }
                        }
                    }
                }
            }
        }
    }
}

class C255b
{
    void f4()
    {
        class C2
        {
            void f3()
            {
                void f2()
                {
                    class C1
                    {
                        void f1()
                        {
                            void f0()
                            {
                                class C0
                                {
                                    void test255b()
                                    {
                                        f4();
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
