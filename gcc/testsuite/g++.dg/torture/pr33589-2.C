// { dg-do compile }

void f(void*) throw();

void somefunction()
{
try {
   void (*g)(void*) = (void (*)(void*))f;
   void (*g2)(int*) = (void (*)(int*))g;
    g2(0);
} catch (...)
{throw;}
}
