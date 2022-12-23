// PR tree-optimization/108137
// { dg-do compile }
// { dg-options "-Wformat-overflow" }

void f(unsigned short x_port, unsigned int x_host)
{
    __builtin_printf("missing %s", x_port ? "host" : &"host:port"[x_host ? 5 : 0]);
}
