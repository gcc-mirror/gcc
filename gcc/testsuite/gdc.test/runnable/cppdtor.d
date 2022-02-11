/*
https://issues.dlang.org/show_bug.cgi?id=21693

RUN_OUTPUT:
---
CppA:
1: CppA.~this
CppB:
2: CppB.~this
2: CppA.~this
CppC:
3: CppC.~this
3: CppB.~this
3: CppA.~this
CppC:
4: CppC.~this
4: CppB.~this
4: CppA.~this
CppNoDestruct:
DA:
1: DA.~this
DB:
2: DB.~this
2: DA.~this
DC:
3: DC.~this
3: DB.~this
3: DA.~this
DC:
4: DC.~this
4: DB.~this
4: DA.~this
---
*/

extern (C) int printf(scope const char*, ...);

extern (C++) class CppA
{
	int num;
	this(int num)
	{
		this.num = num;
	}

	~this()
	{
		printf("%d: CppA.~this\n", num);
	}
}

extern (C++) class CppB : CppA
{
	this(int num)
	{
		super(num);
	}

	~this()
	{
		printf("%d: CppB.~this\n", num);
	}
}

extern (C++) class CppC : CppB
{
	this(int num)
	{
		super(num);
	}

	~this()
	{
		printf("%d: CppC.~this\n", num);
	}
}

extern (D) class DA
{
	int num;
	this(int num)
	{
		this.num = num;
	}

	~this()
	{
		printf("%d: DA.~this\n", num);
	}
}

extern (D) class DB : DA
{
	this(int num)
	{
		super(num);
	}

	~this()
	{
		printf("%d: DB.~this\n", num);
	}
}

extern (D) class DC : DB
{
	this(int num)
	{
		super(num);
	}

	~this()
	{
		printf("%d: DC.~this\n", num);
	}
}

extern (C++) class CppNoDestruct
{
	int num;
	this(int num)
	{
		this.num = num;
	}
}

void main()
{
	printf("CppA:\n"); { scope a = new CppA(1);			}
	printf("CppB:\n"); { scope CppA b = new CppB(2);	}
	printf("CppC:\n"); { scope CppA c = new CppC(3);	}
	printf("CppC:\n"); { scope CppB c2 = new CppC(4);	}

	printf("CppNoDestruct:\n");
	{
		scope const nd = new CppNoDestruct(1);
	}

	printf("DA:\n"); { scope a = new DA(1);		}
	printf("DB:\n"); { scope DA b = new DB(2);	}
	printf("DC:\n"); { scope DA c = new DC(3);	}
	printf("DC:\n"); { scope DB c2 = new DC(4);	}
}
