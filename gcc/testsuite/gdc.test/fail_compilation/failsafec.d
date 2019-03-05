
void delegate() @system sysdelegate;

@safe
void callingsystem()
{
    sysdelegate();
}

