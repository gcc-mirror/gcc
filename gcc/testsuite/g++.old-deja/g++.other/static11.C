// Build don't link:
// Origin: Raja R Harinath <harinath@cs.umn.edu>

enum ReservedName {
  rIGNORE,
  rINCLUDE
};

void maybeStatusKeyword()
{
  static const ReservedName statusKeywords[] = { rINCLUDE, rIGNORE };
  for (int i = 0; i < 2; i++) {
    ReservedName r = statusKeywords[i];
  }
}
