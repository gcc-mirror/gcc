// { dg-do assemble  }
// GROUPS passed conversions
// cvt file
// From: dak@pool.informatik.rwth-aachen.de
// Date:     Sun, 21 Nov 93 17:40:32 +0100
// Subject:  g++ mixes up array dimensions with new
// Message-ID: <9311211640.AA11787@messua>

int main()
{
        int (*a)[5] = new int[6][5];
}
