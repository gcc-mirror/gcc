// { dg-lto-do assemble }
// { dg-lto-options { { -O -flto } } }

template <typename scalar_t, unsigned length>
struct builtin_simd
{
  using type [[gnu::vector_size(sizeof(scalar_t) * length)]] = scalar_t;
};

struct simd_traits
{
  using scalar_type = int;

  template <typename new_scalar_type>
  using rebind = typename builtin_simd<new_scalar_type, 1>::type;
};

template <typename simd_t>
constexpr simd_t fill(typename simd_traits::scalar_type const scalar)
{
  return simd_t{scalar};
}

class Test
{
    using score_type = typename builtin_simd<int, 1>::type;
    score_type data[1]{fill<score_type>(8)};
};

struct TestFactoryBase
{
  virtual Test *CreateTest() = 0;
};

template <class TestClass>
struct TestFactoryImpl : public TestFactoryBase
{
  Test *CreateTest() override { return new TestClass; }
};

void MakeAndRegisterTestInfo(TestFactoryBase *factory);

int main() {
  MakeAndRegisterTestInfo(new TestFactoryImpl<Test>);
}
