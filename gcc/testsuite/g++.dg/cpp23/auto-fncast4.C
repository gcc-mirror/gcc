// PR c++/103049
// P0849R8 - auto(x)
// { dg-do compile { target c++23 } }

class cmdline_parser
{
    public:
    cmdline_parser(char const*);

    auto add_option(char const*, char const*) & -> cmdline_parser &;
    auto add_option(char const*, char const*) && -> cmdline_parser &&;

    void parse(int, char**);
};

int main(int argc, char *argv[])
{
    auto cmdline = cmdline_parser("driver");

    cmdline.add_option("-h", "show help messages")
           .add_option("-v", "show version");

    auto internal = auto(cmdline).add_option("--logging-level", "set logging level to 1-3")
                                 .add_option("--dump-full", "do not minimize dump");
    internal.parse(argc, argv);
}
