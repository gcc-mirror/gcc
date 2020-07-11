// RUNNABLE_PHOBOS_TEST
// REQUIRED_ARGS: -g

import std.string;

string getEnum(size_t count)
{
    string en;
    en ~= "enum KeyCode\n {    \n";

    foreach (i; 0 .. count)
    {
        en ~= format("    memb_%s = %s,\n", i+1, i+1);
    }

    en ~= "} ";
    return en;
}

// Linker warning: Warning 161: Unknown CV version, ignored
// mixin(getEnum(1024));

// ICE
mixin(getEnum(1087));

void main() { }

