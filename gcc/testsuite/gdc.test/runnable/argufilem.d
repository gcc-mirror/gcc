// EXTRA_SOURCES: imports/argufile.d

// NOTE: The bug only works when main.d and argufile.d are put in
//                      separate files and compiled like 'dmd main.d argufile.d'
//                      Also, I'm sure writefln is causing the crash cause when I
//                      use printf(), it doesn't crash.

// main.d -------------------------------------------------------

import argufile;

int main(string[] args)
{
        string message = arguments("bob is ", 7, " years old");

        writefln(message);

        argufile.useargs(); // will crash here

        return 0;
}

