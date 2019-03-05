// https://issues.dlang.org/show_bug.cgi?id=15762

enum Windows1252Char : ubyte { init }

void main() @safe {
	ubyte[] a = [1, 2, 3, 4];
	auto aw = cast(Windows1252Char[]) a;
	auto caw = cast(const(Windows1252Char)[]) a;
	const(ubyte)[] c = [1, 2, 3, 4];
	auto d = cast(const(ubyte)[]) c;
	auto e = cast(const(Windows1252Char)[]) c;
}

