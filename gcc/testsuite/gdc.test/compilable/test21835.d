// https://github.com/dlang/dmd/issues/21832
// resize array of implicitly non-copyable items

void bugGH21832()
{
    static struct S { @disable this(this); }
    static struct Item { S s; }

    Item[] arr;
    arr.length++;
}
