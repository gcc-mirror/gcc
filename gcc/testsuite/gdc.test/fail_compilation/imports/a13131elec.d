module imports.a13131elec;

import imports.a13131checkpoint;        // [2] import

void initElec(T)(T L)
{
    immutable cv = econn.velocities;    // econn is invalid so generates ErrorExp
}

alias econn = elecConnOf!gconn; // invalid declaration
