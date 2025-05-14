#include <string>
struct snd {
  static snd& instance ()
  {
    static snd self;
    return self;
  }
  std::string data;
};
