// Origin: Jeff Donner <jdonner@schedsys.com>
#include <bitset>

int main()
{
  bitset<sizeof(int) * 8> bufWord;

  bufWord[3] = 0;
}
