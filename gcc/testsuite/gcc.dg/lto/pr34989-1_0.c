/* { dg-lto-do link } */

extern struct globals *const ptr_to_globals;
struct globals { };
int syslogd_main(int argc, char **argv)
{
 (*(struct globals**)&ptr_to_globals) = 0;
}

int main() { return 0; }
