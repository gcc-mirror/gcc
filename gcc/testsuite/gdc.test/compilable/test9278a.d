// PERMUTE_ARGS:

// Works fine here
struct datum { float num = 0.0; }

datum emitOne()
{
    datum t;
    return t;
}
const dataArr = [emitOne()];

// A very bad day
//struct datum { float num = 0.0; }

void main(){}
