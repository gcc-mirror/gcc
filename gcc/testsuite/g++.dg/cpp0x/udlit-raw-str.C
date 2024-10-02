// { dg-do compile { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }

#include <string>

std::string operator ""_i18n(const char*, std::size_t);

std::string vogon_poem = R"V0G0N(
                O freddled gruntbuggly thy micturations are to me
                    As plured gabbleblochits on a lurgid bee.
                 Groop, I implore thee my foonting turlingdromes.   
              And hooptiously drangle me with crinkly bindlewurdles,
  Or I will rend thee in the gobberwarts with my blurlecruncheon, see if I don't.

                    (by Prostetnic Vogon Jeltz; see p. 56/57)
)V0G0N"_i18n;
