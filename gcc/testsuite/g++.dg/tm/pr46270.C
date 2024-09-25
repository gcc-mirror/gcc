// { dg-do compile }
// { dg-options "-fgnu-tm" }
// { dg-skip-if "requires hosted libstdc++ for list" { ! hostedlib } }

#include <list>
class Game
{
public:
  struct BuildProject
  {
    int posX;
  };
  std::list<BuildProject> buildProjects;
};

static Game game;
static std::list<std::list<Game::BuildProject>::iterator> erasableBuildProjects;

static void *buildProjectSyncStepConcurrently(int id, int localTeam)
{
  __transaction_relaxed {
    std::list<std::list<Game::BuildProject>::iterator>::iterator it
      = erasableBuildProjects.begin();
    game.buildProjects.erase( (std::list<Game::BuildProject>
			       ::iterator) *it);
  }
  return 0;
}
