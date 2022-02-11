// https://issues.dlang.org/show_bug.cgi?id=21424

void main()
{
    ubyte[10] buf;
    size_t pos = 0;
    size_t num = 5;
    buf[pos++] += num;
    assert(pos == 1);
    assert(buf[0] == 5);
    assert(buf[1] == 0);
}
