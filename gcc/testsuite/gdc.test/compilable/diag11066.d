// REQUIRED_ARGS: -w -profile
void main()
{
    string s;
    foreach (dchar c; s) // affected by dchar
        return;
}
