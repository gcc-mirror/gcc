// Build don't link:
// Origin: Geoffrey Furnish <furnish@actel.com>

namespace N {

    template<class T> class C
    {
        template<class U> friend class C;
    };

}
