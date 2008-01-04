/* { dg-do compile } */

class CString
{
public:
    CString();
    ~CString() { operator delete(_rep); }
    operator const char*() const { return _rep; }
private:
    CString(char* cstr);
    char* _rep;
};

class String
{
public:

    String();
    String(const char* str);
    ~String();
    CString getCString() const;
};

int is_absolute_path(const char *path);

inline void getAbsolutePath(
    const char* path,
    const String& filename)
{
    (!is_absolute_path(filename.getCString()) && path);
    return;
}

int foo(int &value);

int main(int argc, char** argv)
{
    int repeatTestCount = 0;
    if (foo(repeatTestCount))
    {
        repeatTestCount = 1;
    }
    for (int numTests = 1; numTests <= repeatTestCount; numTests++)
    {
            getAbsolutePath("blah", "blah");
    }
    return 0;
}
