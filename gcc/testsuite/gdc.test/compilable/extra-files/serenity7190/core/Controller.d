class Controller {
    mixin template register(T : Controller) {
        enum _s_pkg = __traits(parent, __traits(parent, __traits(parent, T))).stringof["package ".length .. $];

        enum _s_model = T.stringof[0 .. $-`Controller`.length] ~ `Model`;
        mixin(q{enum _ = is(} ~ _s_pkg ~ q{.models.} ~ _s_model ~ q{.} ~ _s_model ~ q{ : serenity7190.core.Model.Model);});
    }
}