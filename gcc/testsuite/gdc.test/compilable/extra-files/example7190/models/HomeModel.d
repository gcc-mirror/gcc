module example7190.models.HomeModel;

import serenity7190.core.Model;

struct Article {
    ulong id;
}

class HomeModel : Model {
    private SqlitePersister!Article mArticles;
}