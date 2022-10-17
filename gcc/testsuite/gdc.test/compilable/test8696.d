// REQUIRED_ARGS: -w

// https://issues.dlang.org/show_bug.cgi?id=8696
// incorrect dangling else with version():
version (all):

version (linux)
{
}
else version (OSX)
{
}
else
{
}
