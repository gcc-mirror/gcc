// Bug: numeric_outputed_need_bar is not cleared properly, adding random '_'s
// to mangled names.

// Build don't link:

template <int seed_length>
class rand1
{
public:
    rand1 ();
};

class codes
{
public:
    rand1<32> * randgen;
    codes (int ptr);

};

codes::codes (int ptr)
{
}
