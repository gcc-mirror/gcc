// https://bugzilla.gdcproject.org/show_bug.cgi?id=198
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

struct S198a
{
    union
    {
        float[3] v;
        struct
        {
            float x;
            float y;
            float z;
        }
    }

    this(float x_, float y_, float z_)
    {
        x = x_;
        y = y_;
        z = z_;
    }

    ref S198a opOpAssign(string op)(S198a operand)
    if (op == "+")
    {
        x += operand.x;
        y += operand.y;
        z += operand.z;
        return this;
    }
}

struct S198b
{
    @property get()
    {
        union Buf
        {
            void[0] result;
        }
        const Buf buf = { };
        return buf.result;
    }
}

struct S198c
{
    @property get()
    {
        union Buf
        {
            TypeInfo info;
            void[0] result;
        }
        const Buf buf = { };
        return buf.result;
    }
}


void main()
{
    S198a sum = S198a(0, 0, 0);

    foreach(size_t v; 0 .. 3)
        sum += S198a(1, 2, 3);

    assert(sum.v == [3, 6, 9]);
}
