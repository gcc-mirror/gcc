// REQUIRED_ARGS: -w

// 8696: incorrect dangling else with version():
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
