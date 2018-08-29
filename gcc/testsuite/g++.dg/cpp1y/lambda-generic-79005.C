// PR c++/79005
// { dg-do compile { target c++14 } }

int main()
{
  auto glambda = [] (auto a)
      {
        const int c = a;
        auto cglambda = [&] ( auto b )
            { 
              double result;
              result = b * a;
              result = b * c;
              return result;
            };
        cglambda ( 1 );
        a = c;
      };

  glambda( 1 );
}
