#include <vector>
#include <cstdlib>
typedef unsigned short uint16;

namespace base {
    class StringPiece
      {
    public:
	typedef std::size_t size_type;
	size_type size() const { return length_; }
	size_type length_;
      };
}

namespace net {
    class DNSSECKeySet
      {
	bool CheckSignature (const base::StringPiece& name, const
			     base::StringPiece& zone, const
			     base::StringPiece& signature, uint16 rrtype,
			     const std::vector<base::StringPiece>& rrdatas);
      };
}

template <class C> class scoped_array
{
public: typedef C element_type;
	explicit scoped_array(C* p = __null):array_(p) {}
private:   C* array_;
};

namespace net {
    bool DNSSECKeySet::CheckSignature (const base::StringPiece& name,
				       const base::StringPiece& zone, const base::StringPiece& signature,
				       uint16 rrtype, const std::vector<base::StringPiece>& rrdatas)
      {
	unsigned signed_data_len = 0;
	for (std::vector<base::StringPiece>::const_iterator i =
	     rrdatas.begin();
	     i != rrdatas.end(); i++) {
	    signed_data_len += 2;
	    signed_data_len += i->size();
	}
	scoped_array<unsigned char> signed_data(new unsigned
						char[signed_data_len]);
      }
}

