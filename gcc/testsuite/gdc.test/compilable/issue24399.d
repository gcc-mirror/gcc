// REQUIRED_ARGS: -main
// LINK:
template rt_options()
{
    __gshared string[] rt_options = [];
    string[] rt_options_tls = [];
}

alias _ = rt_options!();
