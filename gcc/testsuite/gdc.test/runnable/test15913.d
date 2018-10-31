void main()//test10282()
{
  //const     int[3] a4 = [1, 3, 6] * 3;
    immutable int[3] a5 = [1, 3, 6] * 3;

    assert(a5[0] == 3 && a5[1] == 9 && a5[2] == 18);
}
