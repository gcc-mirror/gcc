// PR c++/7050
// { dg-do run }

extern "C" void abort(void);

#define CI(stmt) try { stmt; abort(); } catch (int) { }

struct has_destructor
{
    ~has_destructor() { }
};

struct no_destructor
{
};

int PI(int& i) { return i++; }

int main(int argc, char *argv[])
{
    (argc+1 ? has_destructor() : throw 0);
    CI((argc+1 ? throw 0 : has_destructor()));
    CI((0 ? has_destructor() : throw 0));
    CI((1 ? throw 0 : has_destructor()));
    (0 ? throw 0 : has_destructor());
    (1 ? has_destructor() : throw 0);

    (argc+1 ? no_destructor() : throw 0);
    CI((argc+1 ? throw 0 : no_destructor()));
    CI((0 ? no_destructor() : throw 0));
    CI((1 ? throw 0 : no_destructor()));
    (0 ? throw 0 : no_destructor());
    (1 ? no_destructor() : throw 0);

    int i = 1;
    CI(throw PI(i));
    if (i != 2) abort();

    (1 ? 0 : throw PI(i));
    if (i != 2) abort();

    CI(0 ? 0 : throw PI(i));
    if (i != 3) abort();

    CI(0 ? has_destructor() : throw PI(i));
    if (i != 4) abort();
    (argc+1 ? has_destructor() : throw PI(i));
    if (i != 4) abort();

    i = 1;
    CI(throw i++);
    if (i != 2) abort();

    (1 ? 0 : throw i++);
    if (i != 2) abort();

    CI(0 ? 0 : throw i++);
    if (i != 3) abort();

    CI(0 ? has_destructor() : throw i++);
    if (i != 4) abort();
    (argc+1 ? has_destructor() : throw i++);
    if (i != 4) abort();
}
