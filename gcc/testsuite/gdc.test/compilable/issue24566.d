// https://issues.dlang.org/show_bug.cgi?id=24566

void test24566a()
{
    enum a = true;
    bool b = true;
    enum str = "a";
    if (a && str.length > 1 && str[1] == 'a') {}
    if (b && str.length > 1 && str[1] == 'a') {}
    if (!b && str.length > 1 && str[1] == 'a') {}
    if (str.length > 1 && b && str[1] == 'a') {}
}

void test24566b()
{
    enum a = false;
    bool b = false;
    enum str = "a";
    if (a || str.length <= 1 || str[1] == 'a') {}
    if (b || str.length <= 1 || str[1] == 'a') {}
    if (!b || str.length <= 1 || str[1] == 'a') {}
    if (str.length <= 1 || b || str[1] == 'a') {}
}
