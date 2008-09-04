/* { dg-do compile } */
typedef enum RW { rwBitmapGrey, rwBitmapGrey16 } RW;
void FindDepth(RW);
void ParseDumpBitmap(RW kind, int maxfiles) 
{
    static const RW normalTypes[] = { };
    const RW *bitmapTypes;
    int i;
    switch (kind) {
        case rwBitmapGrey:
        case rwBitmapGrey16:
            bitmapTypes = &kind;
            break;
        default:
            bitmapTypes = normalTypes;
    }
    for (i = 0; i < maxfiles; i++)
        FindDepth(bitmapTypes[i]);
}
