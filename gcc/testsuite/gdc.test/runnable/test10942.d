// REQUIRED_ARGS: -g

string getEnum(size_t count)
{
    string en;
    en ~= "enum KeyCode\n {    \n";

    foreach (i; 1 .. count + 1)
    {
        char[4] buffer;
        int start = buffer.length;

        while (i > 0)
        {
            buffer[--start] = cast(char) ('0' + (i % 10));
            i /= 10;
        }
        char[] id = buffer[start .. $];
        en ~= "memb_" ~ id ~ " = " ~ id ~ ",\n";
    }

    en ~= "} ";
    return en;
}

// Linker warning: Warning 161: Unknown CV version, ignored
// mixin(getEnum(1024));

// ICE
mixin(getEnum(1087));

void main() { }
