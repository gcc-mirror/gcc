// PR c++/95241
// { dg-do compile { target c++11 } }

struct Fragment
{
  int off;
  constexpr Fragment(int _off) : off(_off) { }
  constexpr Fragment() : Fragment(1) { }
};

struct Field
{
  Fragment fragments[3];
  constexpr Field(int off) : fragments{{off}} { }
};

constexpr Field field{0};

static_assert(field.fragments[0].off == 0
	      && field.fragments[1].off == 1
	      && field.fragments[2].off == 1, "");
