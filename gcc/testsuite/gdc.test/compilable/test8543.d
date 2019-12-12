
version (D_SIMD)
{
    struct vfloat
    {
    public:
        __vector(float[4]) f32;

        this(float X) nothrow
        {
            f32.ptr[0] = X;
            f32.ptr[1] = X;
            f32.ptr[2] = X;
            f32.ptr[3] = X;
        }
        this(float X, float Y, float Z, float W) nothrow
        {
            f32.array[0] = X;
            f32.array[1] = Y;
            f32.array[2] = Z;
            f32.array[3] = W;
        }
        this(float[4] values) nothrow
        {
            f32.array = values;
        }
    }

    immutable GvfGlobal_ThreeA = vfloat(3.0f);
    immutable GvfGlobal_ThreeB = vfloat(3.0f, 3.0f, 3.0f, 3.0f);
    immutable GvfGlobal_ThreeC = vfloat([3.0f, 3.0f, 3.0f, 3.0f]);
}
