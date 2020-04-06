static if (__traits(allMembers, __traits(parent,{}))[0]=="object") {
	enum test = 0;
}

static foreach (m; __traits(allMembers, __traits(parent,{}))) {
	mixin("enum new"~m~"=`"~m~"`;");
}

static assert([__traits(allMembers, __traits(parent,{}))] == ["object", "test", "newobject", "newWorld", "newBuildStuff", "World", "BuildStuff"]);

struct World {
	mixin BuildStuff;
}

template BuildStuff() {
	static foreach(elem; __traits(allMembers, typeof(this))) {}
}
