// https://bugzilla.gdcproject.org/show_bug.cgi?id=191
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

class C191
{
    int count = 0;

    void testA()
    {
        class Inner
        {
            void test()
            {
                void localFunction()
                {
                    if (++count != 5)
                        testA();
                }
                localFunction();
            }
        }
        scope ic = new Inner();
        ic.test();
    }

    void testB()
    {
        class Inner
        {
            void test()
            {
                void localFunction()
                {
                    void anotherLocalFunction()
                    {
                        if (++count != 10)
                            testB();
                    }
                    anotherLocalFunction();
                }
                localFunction();
            }
        }
        scope ic = new Inner();
        ic.test();
    }

    void testC()
    {
        class Inner
        {
            int a = 1;

            void test()
            {
                void localFunction()
                {
                    count += a;
                    if (count != 15)
                        testC();
                    assert(a == 1);
                }
                localFunction();
            }
        }
        scope ic = new Inner();
        ic.test();
    }

    void testD()
    {
        class Inner
        {
            void test()
            {
                int a = 1;

                void localFunction()
                {
                    count += a;
                    if (count != 20)
                        testD();
                    assert(a == 1);
                }
                localFunction();
            }
        }
        scope ic = new Inner();
        ic.test();
    }

    void testE()
    {
        class Inner
        {
            int a = 1;

            void test()
            {
                void localFunction()
                {
                    void anotherLocalFunction()
                    {
                        count += a;
                        if (count != 25)
                            testE();
                        assert(a == 1);
                    }

                    anotherLocalFunction();
                }

                localFunction();
            }
        }
        scope ic = new Inner();
        ic.test();
    }

    void testF()
    {
        class Inner
        {
            void test()
            {
                int a = 1;

                void localFunction()
                {
                    void anotherLocalFunction()
                    {
                        count += a;
                        if (count != 30)
                            testF();
                        assert(a == 1);
                    }

                    anotherLocalFunction();
                }

                localFunction();
            }
        }
        scope ic = new Inner();
        ic.test();
    }

    void testG()
    {
        class Inner
        {
            void test()
            {
                void localFunction()
                {
                    int a = 1;

                    void anotherLocalFunction()
                    {
                        count += a;
                        if (count != 35)
                            testG();
                        assert(a == 1);
                    }

                    anotherLocalFunction();
                }

                localFunction();
            }
        }
        scope ic = new Inner();
        ic.test();
    }
}

void main()
{
    scope oc = new C191();
    oc.testA();
    assert(oc.count == 5);

    oc.testB();
    assert(oc.count == 10);

    oc.testC();
    assert(oc.count == 15);

    oc.testD();
    assert(oc.count == 20);

    oc.testE();
    assert(oc.count == 25);

    oc.testF();
    assert(oc.count == 30);

    oc.testG();
    assert(oc.count == 35);
}
