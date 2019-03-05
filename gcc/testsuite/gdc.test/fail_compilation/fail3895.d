import std.stdio;

void main() {
    double[] stuff = [1.,2.,3.,4.,5.];
    float[] otherStuff;
    otherStuff ~= stuff;
    writeln(otherStuff);
}

