import core.interpolation;

alias AliasSeq(T...) = T;

string simpleToString(T...)(T thing) {
    string s;
    foreach(item; thing)
        // all the items provided by core.interpolation have
        // toString to return an appropriate value
        //
        // then this particular example only has embedded strings
        // and chars, to we can append them directly
        static if(__traits(hasMember, item, "toString"))
            s ~= item.toString();
        else
            s ~= item;

    return s;
}

void main() {
	int a = 1;
	string b = "one";
	// parser won't permit alias = i".." directly; i"..." is meant to
	// be used as a function/template parameter at this time.
	alias expr = AliasSeq!i"$(a) $(b)";
	// elements from the source code are available at compile time, so
	// we static assert those, but the values, of course, are different
	static assert(expr[0] == InterpolationHeader());
	static assert(expr[1] == InterpolatedExpression!"a"());
	assert(expr[2] == a); // actual value not available at compile time
	static assert(expr[3] == InterpolatedLiteral!" "());
	// the parens around the expression are not included
	static assert(expr[4] == InterpolatedExpression!"b"());
	assert(expr[5] == b); // actual value not available at compile time
	static assert(expr[6] == InterpolationFooter());

	// it does currently allow `auto` to be used, it creates a value tuple
	// you can embed any D expressions inside the parenthesis, and the
	// token is not ended until you get the *outer* ) and ".
	auto thing = i"$(b) $("$" ~ ')' ~ `"`)";
	assert(simpleToString(thing) == "one $)\"");

        assert(simpleToString(i"$b") == "$b"); // support for $ident removed by popular demand

        // i`` and iq{} should also work
        assert(simpleToString(i` $(b) is $(b)!`) == " one is one!");
        assert(simpleToString(iq{ $(b) is $(b)!}) == " one is one!");
        assert(simpleToString(i`\$('$')`) == "\\$"); // no \ escape there
        assert(simpleToString(iq{{$('$')}}) == "{$}"); // {} needs to work
}
